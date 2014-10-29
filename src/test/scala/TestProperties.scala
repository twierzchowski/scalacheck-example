import org.scalacheck._
import org.scalacheck.Prop._
import Gen._

object BasicProperties extends Properties("basic examples") {
	
	property("a+b == b+a") = forAll{
		(a: Int, b: Int) => a+b == b+a
	}
	
	property("exists i*i == i") = exists{
		(i: Int) => i*i == i
	}

	property("exists i*3 == 33") = exists{
		(i: Int) => i*3 == 33
	}
}

object SqrProperties extends Properties("sqr examples") {
	property("sqr") = forAll(posNum[Int]){		//forAll(IntGen) //forAll(posNum[Int])
		(n:Int) => n*n > 0
	}

	property("sqr+sqrt") = forAll(posNum[Int]){
		(n:Int) => scala.math.sqrt(n*n) == n
	}

}

object FactProperties extends Properties("factorial properties") {

	property("fact 1") = forAll(Gen.choose(0,20)){
		(n:Int) => UsefulMethods.factorial(n)>0
	}
	
	def factorial(n: BigInt): BigInt = {
		def fact1(n: BigInt, acc: BigInt): BigInt = {	
			if (n == 0 ) acc
			else fact1(n-1, n*acc)
		}
		fact1(n, 1)
	}

//	property("fact 2") = forAll{
//		(n:Int) => UsefulMethods.factorial(n) == factorial(n)
//	}
}

object CipherProperties extends Properties("cipher properties") {

	// przyklad z szyforwanie cezara
	//property("roundtrip") = forAll { 
	//	(a:String, b:Int) =>
	//	UsefulMethods.cipher(UsefulMethods.cipher(a,b),-b) == a 
	//}

	
	val newGenerator: Gen[(String, Int)] = for {
		str <- listOf(alphaLowerChar).map(_.mkString)
		shift <- Gen.choose(Int.MinValue, Int.MaxValue)
	} yield (str, shift)
	

	def isAlphaString(s:String): Boolean = {
		s.forall(chr => chr>='a' && chr<='z')
	}

	property("encrypted text should be alphastring") = forAll(newGenerator) { 
		input: (String, Int) =>	
		isAlphaString(UsefulMethods.cipher(input._1, input._2))
	}
	
	property("roundtrip alfastrings") = forAll(newGenerator) { 
		input: (String, Int) =>
//		collect(input) 
		{ UsefulMethods.cipher(UsefulMethods.cipher(input._1,input._2),-input._2) == input._1 }
	}
}


object TestProperties {
	def main(args: Array[String]) {
		BasicProperties.check
		SqrProperties.check
		FactProperties.check		
		CipherProperties.check
	}
}
