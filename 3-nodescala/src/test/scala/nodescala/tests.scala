package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("all should return results in correct order") {

    val f1 = Future(1)
    val f2 = Future(2)
    val f3 = Future(3)
    val allFs = Future.all(List(f1, f2, f3))

    val results = Await.result(allFs, 100 millis)

    assert(results === List(1, 2, 3))

  }

  test("all should return failed future if one of original futures fails") {

    val f1 = Future(1)
    val f2 = Future(throw new NullPointerException)
    val f3 = Future(3)
    val allFs = Future.all(List(f1, f2, f3))

    try {
      Await.result(allFs, 100 millis)
      assert(false)
    } catch {
      case n: NullPointerException => // ok!
    }

  }

  test("any should return first completed future") {

    val f1 = Future {
      blocking {
        Thread.sleep(10)
      }
      1
    }
    val f2 = Future {
      2
    }
    val f3 = Future {
      blocking {
        Thread.sleep(10)
      }
      3
    }
    val anyFs = Future.any(List(f1, f2, f3))

    val result = Await.result(anyFs, 30 millis)
    assert(result === 2)

  }

  test("any should return failed future if first completed future fails") {

    val f1 = Future {
      blocking {
        Thread.sleep(10)
      }
      1
    }
    val f2 = Future {
      throw new NullPointerException
    }
    val f3 = Future {
      blocking {
        Thread.sleep(10)
      }
      3
    }
    val anyFs = Future.any(List(f1, f2, f3))

    try {
      Await.result(anyFs, 30 millis)
      assert(false)
    } catch {
      case e: NullPointerException => // ok!
    }

  }

  test("delay should produce future that completes after certain duration") {

    val delayFuture = Future.delay(10 millis)

    try {
      Await.ready(delayFuture, 2 millis)
      assert(false, "delayFuture should  not complete earlier than the timeout")
    } catch {
      case e: TimeoutException => // ok!
    }

    Await.ready(delayFuture, 20 millis)

  }

  test("now should return computed value if future is completed") {

    val fut = Future("completed")
    Await.ready(fut, 10 millis)

    assert(fut.now === "completed")

  }

  test("now should throw NoSuchElementException if future is not completed yet") {

    val fut = Future {
      blocking {
        Thread.sleep(10)
      }
      "completed"
    }

    try {
      fut.now
      assert(false, "now did not throw NoSuchElementException")
    } catch {
      case e: NoSuchElementException => // ok!
    }

  }

  test("now should throw NoSuchElementException if future completed with failure") {

    val fut = Future(throw new NullPointerException)
    Await.ready(fut, 10 millis)

    try {
      fut.now
      assert(false, "now did not throw NoSuchElementException")
    } catch {
      case e: NoSuchElementException => // ok!
    }

  }

  test("continueWith should apply cont function after completion of this future") {

    val fut1 = Future {
      blocking {
        Thread.sleep(50)
      }
      1
    }

    val fut2 = fut1.continueWith[Int](_.value.get.get + 1)

    val result = Await.result(fut2, 100 millis)

    assert(result === 2)

  }

  test("continue should apply cont function after successful completion of this future") {

    val fut1 = Future(1)
    val fut2 = fut1.continue({
      case Success(v) => v+1
      case Failure(thr) => throw thr
    })

    val result = Await.result(fut2, 100 millis)

    assert(result === 2)

  }

  test("continue should apply cont function after failure of this future") {

    val fut1 = Future[Int](throw new NullPointerException)
    val fut2 = fut1.continue({
      case Success(v) => 1
      case Failure(err) => 2
    })

    Await.ready(fut2, 50 millis)
    assert(fut2.value.get.get === 2)

  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("run should allow stopping the computation") {

    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          println("working")
        }
        println("done")
      }
    }
    Future.delay(100 millis) onSuccess {
      case _ => working.unsubscribe()
    }

  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

  test("Server should cancel long running response") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => new Iterator[String] {
        override def hasNext: Boolean = true
        override def next(): String = ""
      }
    }

    // wait until server is really installed
    Thread.sleep(500)

    val webpage = dummy.emit("/testDir", immutable.Map.empty)

    dummySubscription.unsubscribe()

    // should not throw exception
    Await.ready(webpage.loaded.future, 1 second)

  }

}




