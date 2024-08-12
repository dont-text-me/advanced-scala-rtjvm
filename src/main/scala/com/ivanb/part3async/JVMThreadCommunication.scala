package com.ivanb.part3async

import scala.collection.mutable
import scala.util.Random

object JVMThreadCommunication {
  def main(args: Array[String]): Unit = {
    ProdConsV4.start(3, 3, 5)
  }

}

class SimpleContainer {
  private var value: Int = 0

  def isEmpty: Boolean = value == 0
  def set(newValue: Int): Unit = value = newValue
  def get: Int = {
    val result = value
    value = 0
    result
  }
}

object ProdConsV1 {
  def start(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while (container.isEmpty) { // busy wait
        println("[consumer] waiting for a value...")
      }

      println(s"[consumer] consumed value: ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println(s"[producer] producing the value $value")
      container.set(value)
    })

    consumer.start()
    producer.start()
  }
}

object ProdConsV2 {
  def start(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized { // block all other threads trying to lock this object
        // thread safe code
        if (container.isEmpty)
          container.wait() // release the lock on the object, suspend the current thread
          // NOTE: if the producer finishes way before this part, there will be no one to release the lock.
        // The if statement guards against such cases
      }
      println(s"[consumer] consumed value: ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42

      container.synchronized {
        println(s"[producer] producing the value $value")
        container.set(value)
        container
          .notify() // awaken ONE (no guarantee as to which one) suspended thread on the object i.e. the consumer
      }

    })

    consumer.start()
    producer.start()
  }
}

object ProdConsV3{
  def start(containerCapacity: Int) : Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]()

    val consumer = new Thread(() => {
      val random = new Random(System.nanoTime())

      while (true){
        buffer.synchronized{
          if(buffer.isEmpty){
            println("[consumer] buffer empty, waiting...")
            buffer.wait()
          }
          // buffer must not be empty
          val x = buffer.dequeue()
          println(s"[consumer] consumed $x")
          buffer.notify() // wake the producer up if it's asleep (or no op if it isnt)
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random(System.nanoTime())
      var counter = 0

      while (true){
        buffer.synchronized{
          if(buffer.size == containerCapacity){
            println("[producer] buffer full, waiting...")
            buffer.wait()
          }
           // buffer not full
           val newElement = counter
           counter += 1
          println(s"[producer] producing $newElement")
          buffer.enqueue(newElement)
          buffer.notify() // let the consumer know if it's waiting for a new element
          // if the consumer is already awake, this will no op
        }
        Thread.sleep(random.nextInt(500))
      }
    })
    consumer.start()
    producer.start()
  }
}

object ProdConsV4 {

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread{
    override def run(): Unit = {
      val random = new Random(System.nanoTime())
      while (true){
        buffer.synchronized{
          while (buffer.isEmpty){
            println(s"[consumer $id] buffer empty, waiting...")
            buffer.wait()
          }
          // buffer non empty
          val newValue = buffer.dequeue()
          println(s"[consumer $id] consumed $newValue")
          buffer.notifyAll()
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }
  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random(System.nanoTime())
      var currentCount = 0
      while (true){
        buffer.synchronized{
          while(buffer.size == capacity){
            println(s"[producer $id] buffer full, waiting...")
            buffer.wait()
          }

          println(s"[producer $id] producing $currentCount")
          buffer.enqueue(currentCount)

          buffer.notifyAll()
          currentCount += 1

        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  def start(nProducers: Int, nConsumers: Int, containerCapacity: Int): Unit = {
    val buffer : mutable.Queue[Int] = new mutable.Queue[Int]()
    val producers = (1 to nProducers).map(id => new Producer(id, buffer, containerCapacity))
    val consumers = (1 to nConsumers).map(id => new Consumer(id, buffer))
    producers.foreach(_.start())
    consumers.foreach(_.start())

  }
}
