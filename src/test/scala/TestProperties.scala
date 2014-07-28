import org.scalacheck._
import org.scalacheck.Prop._
import Gen._

object Factorial {
	def factorial(n: Int): Long = {
	if (n<=1)
		1
	else
		n*factorial(n-1)
	}
}

object CipherProperties extends Properties("cipher properties") {
	property("roundtrip") = forAll { (a:String, b:Int) =>
	UsefulMethods.cipher(UsefulMethods.cipher(a,b),-b) == a }

	val newGenerator: Gen[(String, Int)] = for {
	str <- Gen.alphaStr
	shift <- Gen.choose(0,10)
	} yield (str, shift)

	property("roundtrip alfastrings") = forAll(newGenerator) { input: (String, Int) =>
	UsefulMethods.cipher(UsefulMethods.cipher(input._1,input._2),-input._2) == input._1 }
}

object TestProperties {

	val propPositive = forAll { (n: Int) =>
	UsefulMethods.factorial(n) > 0 }

	val propRecur = forAll(Gen.choose(0,20)) { (n: Int) =>
	UsefulMethods.factorial(n) == Factorial.factorial(n) }

	def main(args: Array[String]) {
		println("Hello world!")
		cipherProperties.check
//		propRecur.check
	}
}
