package com.ivanb.part3async

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success, Try}

object Futures {

  def doSomeComputation(): Int =
    Thread.sleep(1000)
    42
  // thread pool (java-specific)
  val executor = Executors.newFixedThreadPool(4)

  // thread pool (scala-specific)
  given executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(executor)

  // a future = an async computation that will finish at some point
  val aFuture: Future[Int] =
    Future.apply(doSomeComputation()) // given executionContext will be passed here

  // option, because we don't know if there is a value, Try, because computation may fail
  val futureInstantResult: Option[Try[Int]] = aFuture.value

  aFuture.onComplete {
    case Success(value) => println(s"computation done with result $value")
    case Failure(ex)    => println(s"computation failed: $ex")
  } // evaluated on some other thread i.e. no control over where or when these are evaluated

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

  def main(args: Array[String]): Unit = {
    sendMessageToBestFriend3("rtjvm.id.1-alice", "Hello bob")
    Thread.sleep(2000)
    executor.shutdown()
  }

}
