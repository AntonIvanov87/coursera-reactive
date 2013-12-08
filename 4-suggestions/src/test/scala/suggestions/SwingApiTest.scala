package suggestions



import scala.collection._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import scala.swing.event.Event
import scala.swing.Reactions.Reaction
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SwingApiTest extends FunSuite {

  object swingApi extends SwingApi {
    class ValueChanged(val textField: TextField) extends Event

    object ValueChanged {
      def unapply(x: Event) = x match {
        case vc: ValueChanged => Some(vc.textField)
        case _ => None
      }
    }

    class ButtonClicked(val source: Button) extends Event

    object ButtonClicked {
      def unapply(x: Event) = x match {
        case bc: ButtonClicked => Some(bc.source)
        case _ => None
      }
    }

    class Component {
      private val subscriptions = mutable.Set[Reaction]()
      def subscribe(r: Reaction) {
        subscriptions add r
      }
      def unsubscribe(r: Reaction) {
        subscriptions remove r
      }
      def publish(e: Event) {
        for (r <- subscriptions) r(e)
      }
    }

    class TextField extends Component {
      private var _text = ""
      def text = _text
      def text_=(t: String) {
        _text = t
        publish(new ValueChanged(this))
      }
    }

    class Button extends Component {
      def click() {
        publish(new ButtonClicked(this))
      }
    }
  }

  import swingApi._

  test("SwingApi should emit text field values to the observable") {
    val textField = new swingApi.TextField

    val observed = mutable.Buffer[String]()
    textField.textValues subscribe (observed += _)

    textField.text = "T"
    textField.text = "Tu"
    textField.text = "Tur"

    assert(observed == Seq("T", "Tu", "Tur"), observed)
  }

  test("SwingApi should not emit text field values after subscription cancellation") {
    val textField = new swingApi.TextField

    val observed = mutable.Buffer[String]()

    val sub = textField.textValues subscribe (observed += _)

    textField.text = "T"
    textField.text = "Tu"
    sub.unsubscribe()
    textField.text = "Turi"
    textField.text = "Turin"

    assert(observed == Seq("T", "Tu"), observed)
  }

  test("SwingApi should emit button click events to the observable") {
    val button = new swingApi.Button

    var numOfClicks = 0

    button.clicks subscribe (_ => numOfClicks+=1)

    button.click()
    button.click()
    button.click()

    assert(numOfClicks == 3)
  }

  test("SwingApi should not emit button click events after subscription cancellation") {
    val button = new swingApi.Button

    var numOfClicks = 0

    val subscription = button.clicks subscribe (_ => numOfClicks+=1)

    button.click()
    button.click()
    subscription.unsubscribe()
    button.click()
    button.click()

    assert(numOfClicks == 2)

  }
}