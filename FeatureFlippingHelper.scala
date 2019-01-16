import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.Random

class FeatureFlippingHelper(name: String) {

  type FeatureFlippedFunction[I, O] = I => Future[O]

  def handleFeatureFlipping[I, O](isFeatureActivated: Boolean, input: I)
                                 (newFeatureFonction: FeatureFlippedFunction[I, O])
                                 (oldFeatureFonction: FeatureFlippedFunction[I, O]): Future[O] = {
    if (isFeatureActivated)
      newFeatureFonction(input).recoverWith {
        case e =>
          println(s"Error while applying new feature $name - switching to old feature - error: $e")
          oldFeatureFonction(input)
      }
    else oldFeatureFonction(input)
  }

}

object FeatureFlippingTest extends App {

  def newFeatureFunction(input: String): Future[String] = Future(input.concat("_new"))

  def oldFeatureFunction(input: String): Future[String] = Future(input.concat("_old"))

  def newFeatureFunctionFailed(input: String): Future[String] = Future.failed(new NullPointerException)

  val featureHelper = new FeatureFlippingHelper("Migration Database")

  import scala.concurrent.duration._

  val test = featureHelper.handleFeatureFlipping(true, "toto")(newFeatureFunction)(oldFeatureFunction)

  println(Await.result(test, 1.minute))

}

object ABTesting extends App {

  def functionA(input: String): Future[String] = Future(input.concat("_testA"))

  def functionB(input: String): Future[String] = Future(input.concat("_testB"))

  val featureHelper = new FeatureFlippingHelper("A/B Testing concatenation on string")

  import scala.concurrent.duration._

  val test = featureHelper.handleFeatureFlipping(Random.nextBoolean(), "toto")(functionA)(functionB)

  println(Await.result(test, 1.minute))

}
