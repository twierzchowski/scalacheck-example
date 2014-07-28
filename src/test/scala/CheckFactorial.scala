import org.scalacheck._
import org.scalacheck.Prop._
import Gen._
//import org.scalacheck.Test.check


object CheckFactorial {

def factorial(n: Int): Long = {
	if (n<=1)
		1
	else
		n*factorial(n-1)
}


//val propRecur = forAll(Gen.choose(0,1000)) { a: Int =>
//	UsefulMethods.factorial(a) == factorial(a) }

//val propMakeList = forAll { (n: Int) =>
//	(n>=0 && n<2000) ==> (List.fill(n)("").length == n )
//}

def main(args: Array[String]) = 
	//tests foreach { case (name, p) => check(p) }
//	propRecur.check
	println(factorial(5))
	println(UsefulMethods.factorial(5))

}
