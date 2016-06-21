package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeDelta") {
    assert(Polynomial.computeDelta(Var(1), Var(-2), Var(1))() == 0)
    assert(Polynomial.computeDelta(Var(2), Var(-7), Var(3))() == 25)
    assert(Polynomial.computeDelta(Var(1), Var(1), Var(1))() == -3)
  }

  test("computeSolutions") {
    assert(Polynomial.computeSolutions(Var(1), Var(-2), Var(1), Var(0))() == Set(1))
    assert(Polynomial.computeSolutions(Var(-1), Var(7), Var(-10), Var(9))() == Set(2, 5))
    assert(Polynomial.computeSolutions(Var(1), Var(1), Var(1), Var(-3))() == Set())
  }

  test("eval") {
    assert(Calculator.eval(Literal(10D), Map()) == 10D)
    assert(Calculator.eval(Plus(Literal(10D), Literal(5D)), Map()) == 15D)
    assert(Calculator.eval(Minus(Literal(10D), Literal(5D)), Map()) == 5D)
    assert(Calculator.eval(Times(Literal(10D), Literal(5D)), Map()) == 50D)
    assert(Calculator.eval(Divide(Literal(10D), Literal(5D)), Map()) == 2D)
    assert(Calculator.eval(Times(Ref("test"), Literal(2D)), Map("test" -> Signal { Literal(7D) })) == 14D)
    assert(Calculator.eval(Times(Ref("does_not_exists"), Literal(2D)), Map("test" -> Signal { Literal(7D) })).isNaN)
    assert(Calculator.eval(Times(Ref("cyclic_a"), Literal(2D)),
      Map(
        "cyclic_a" -> Signal { Plus(Ref("cyclic_b"), Literal(1D)) },
        "cyclic_b" -> Signal { Times(Literal(2D), Ref("cyclic_a")) })).isNaN)
  }
}
