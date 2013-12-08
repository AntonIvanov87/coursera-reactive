package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import rx.lang.scala.subscriptions.Subscription
import java.io.FileNotFoundException


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("recovered should wrap normal values in Success") {

    val wrapped = Observable(1, 2, 3).recovered
    assert(wrapped.toBlockingObservable.toList === List(Success(1), Success(2), Success(3)))

  }

  test("recovered should wrap error values in Failure") {

    val someError = new RuntimeException("some runtime exception")
    val observableWithError = Observable.apply[Int](
      (observer: Observer[Int]) => {
        observer.onNext(1)
        observer.onError(someError)
        Subscription()
      }
    )
    val wrapped = observableWithError.recovered
    assert(wrapped.toBlockingObservable.toList === List(Success(1), Failure(someError)))

  }

  test("timedOut should return observable that contains all events from original observable happened before totalSecs") {

    val timedOutObservable = Observable.interval(300 millis).timedOut(1)
    assert(timedOutObservable.toBlockingObservable.toList === List(0, 1, 2))

  }

  test("timedOut should return completed observable if original observable completes before totalSecs") {

    val timedOutObservable = Observable(1, 2, 3).timedOut(1)
    assert(timedOutObservable.toBlockingObservable.toList === List(1, 2, 3))

  }

  test("timedOut should return error observable if original observable completes with error") {

    val origError = new IllegalArgumentException("some message")
    val observableWithError = Observable.apply[Int](
      (observer: Observer[Int]) => {
        observer.onNext(1)
        observer.onError(origError)
        Subscription()
      }
    )

    val timedOut = observableWithError.timedOut(1)
    try {
      timedOut.toBlockingObservable.toList
      fail("exception was not thrown")
    } catch {
      case actualError: IllegalArgumentException => assert(actualError === origError)
    }

  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("concatRecovered should wrap exceptions in Failure") {

    val requests = Observable(1, 2, 3)
    val myException = new RuntimeException("my runtime exception")
    val remoteComputation = (n: Int) => if (n != 2) Observable.apply(n) else Observable(myException)

    val responses = requests concatRecovered remoteComputation

    assert(responses.toBlockingObservable.toList === List(Success(1), Failure(myException), Success(3)))

  }

}