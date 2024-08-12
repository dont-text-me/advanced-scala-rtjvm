package com.ivanb.part3async

import java.util.concurrent.Executors
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Random, Success}

object Futures {

  def doSomeComputation(): Int =
    Thread.sleep(1000)
    42
  // thread pool (java-specific)
  val executor = Executors.newFixedThreadPool(4)

  // thread pool (scala-specific)
  given executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(executor)

//  // a future = an async computation that will finish at some point
//  val aFuture: Future[Int] =
//    Future.apply(doSomeComputation()) // given executionContext will be passed here
//
//  // option, because we don't know if there is a value, Try, because computation may fail
//  val futureInstantResult: Option[Try[Int]] = aFuture.value
//
//  aFuture.onComplete {
//    case Success(value) => println(s"computation done with result $value")
//    case Failure(ex)    => println(s"computation failed: $ex")
//  } // evaluated on some other thread i.e. no control over where or when these are evaluated

  case class Profile(id: String, name: String) {
    def sendMessage(anotherProfile: Profile, message: String): Unit = println(
      s"${this.name} sending to ${anotherProfile.name}"
    )
  }

  object SocialNetwork {
    val names = Map(
      "rtjvm.id.1-alice" -> "Alice",
      "rtjvm.id.2-bob" -> "Bob",
      "rtjvm.id.3-claire" -> "Claire"
    )

    val friends = Map(
      "rtjvm.id.1-alice" -> "rtjvm.id.2-bob"
    )

    val random = new Random()

    def fetchProfile(id: String): Future[Profile] = Future {
      // fetch something from db
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bestFriendId = friends(profile.id)
      Profile(bestFriendId, names(bestFriendId))
    }
  }

  def sendMessageToBestFriend(accountId: String, message: String): Unit = {
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    profileFuture.onComplete {
      case Success(profile) =>
        val friendProfileFuture = SocialNetwork.fetchBestFriend(profile)
        friendProfileFuture.onComplete {
          case Success(friendProfile) => profile.sendMessage(friendProfile, message)
          case Failure(ex)            => ex.printStackTrace()

        }
      case Failure(ex) => ex.printStackTrace()
    }
  }

  def sendMessageToBestFriend2(accountId: String, message: String): Unit = {
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    val bestFriendFuture = profileFuture.flatMap { profile =>
      SocialNetwork.fetchBestFriend(profile).map { bestFriend =>
        profile.sendMessage(bestFriend, message)
      }
    }
  }

  def sendMessageToBestFriend3(accountId: String, message: String): Unit =
    for
      profile <- SocialNetwork.fetchProfile(accountId)
      bestFriend <- SocialNetwork.fetchBestFriend(profile)
    yield profile.sendMessage(bestFriend, message) // same thing, chains of map and flatMap

  val profileNoMatterWhat: Future[Profile] = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("rtjvm.id.0-fallback", "fallback profile")
  }

  val aFetchedProfileNoMatterWhat: Future[Profile] =
    SocialNetwork.fetchProfile("unnown id").recoverWith { case e =>
      SocialNetwork.fetchProfile("rtjvvm-0-dummy")
    }

  val fallbackProfile: Future[Profile] =
    SocialNetwork.fetchProfile("unnown id").fallbackTo(SocialNetwork.fetchProfile("rtjvvm-0-dummy"))

  // blocking for a future
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    def fetchUser(name: String): Future[User] = Future {
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] =
      Future {
        Thread.sleep(1000)
        Transaction(user.name, merchantName, amount, "SUCCESS")
      }

    def purchase(username: String, item: String, merchantName: String, price: Double): String = { // note: not a future
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, price)
      } yield transaction.status
      // block the calling thread until the future is ready
      Await.result(
        transactionStatusFuture,
        2.seconds
      ) // throws TimeoutException if the future doesnt finish in 2s
    }
  }
  // promises a.k.a manually completable futures

  def demoPromises(): Unit =
    val promise = Promise[Int]()
    val futureInside: Future[Int] = promise.future

    // thread 1 - consumer, monitors the future for completion
    futureInside.onComplete {
      case Success(value) => println(s"[consumer] Completed with $value")
      case Failure(ex)    => ex.printStackTrace()
    }

    val producerThread = new Thread(() => {
      println("[producer] Working ...")
      Thread.sleep(1000)
      promise.success(42)
      println("[producer] done")
    })
    producerThread.start()

  def fulfillImmediately[A](value: A): Future[A] = Future(value) // completes as soon as possible
  def fulfillImmediately2[A](value: A): Future[A] =
    Future.successful(value) // creates an *already completed* future (synchronous)

  /** Make sure first Future has been completed before returning the second */
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = first.flatMap(_ => second)

  /** Return the first future to complete */
  def first[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val promise = Promise[A]()
    f1.onComplete(res => promise.tryComplete(res))
    f2.onComplete(res => promise.tryComplete(res))
    promise.future
  }

  /** Return the last future to complete */
  def last[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val bothPromise = Promise[A]()
    val lastPromise = Promise[A]()

    f1.onComplete(res => {
      if !bothPromise.tryComplete(res) then lastPromise.complete(res)
    })
    f2.onComplete(res => {
      if !bothPromise.tryComplete(res) then lastPromise.complete(res)
    })
    lastPromise.future
  }

  def retryUntil[A](action: () => Future[A], predicate: A => Boolean): Future[A] = {
    // my solution
    action().flatMap {
      case x if predicate(x) => Promise[A].success(x).future
      case x if !predicate(x) =>
        println(s"result $x did not pass predicate, retrying...")
        retryUntil(action, predicate)
    }
  }

  def retryUntil2[A](
      action: () => Future[A],
      predicate: A => Boolean
  ): Future[A] = // model solution
    action().filter(predicate).recoverWith { case _ =>
      retryUntil2(action, predicate)
    }

  def main(args: Array[String]): Unit = {

    lazy val futureFirst = Future {
      Thread.sleep(2000)
      42
    }

    lazy val futureSecond = Future {
      Thread.sleep(1000)
      43
    }

    val nums = (1 to 10).iterator

    val futureIncr = () => {
      Future[Int] {
        Thread.sleep(200)
        nums.next()
      }
    }

    retryUntil2(futureIncr, (x: Int) => x >= 6).foreach(println)
    Thread.sleep(5000)
    executor.shutdown()
  }

}
