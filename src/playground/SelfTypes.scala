package playground

object SelfTypes extends App {

  trait Instrumentalist{
    def play() : Unit
  }

  /**
   * self types enforce that however implement singer must also implement Instrumentalist
   * this is useful when there is no relationship between the traits, and it makes no
   * sense of extending (inheritance relationship).
   * while inheritance is a "is a" relationship"
   * the self type is "requires" other type (also useful for cyclic dependency)
   * example:
   */
  trait Singer{ self: Instrumentalist ⇒ // not a lambda, but a self type
    def sing:Unit
  }

  class LeadSinger extends Singer with Instrumentalist{
    override def sing: Unit = println("laaa")

    override def play(): Unit = println("*Guitar playing*")

  }

  // this is also known as the CAKE PATTERN (bake your application in layers)=> "dependency injection"
  // DI - DONE BY SOME FRAMEWORK at runtime
  class Component {
    // API
  }
  class ComponentA extends Component
  class ComponentB extends Component
  class DependentComponent(val component: Component)

  // CAKE PATTERN - types checked at compile type
  trait ScalaComponent {
    // API
    def action(x: Int): String
  }
  trait ScalaDependentComponent { self: ScalaComponent =>
    def dependentAction(x: Int): String = action(x) + " this rocks!"
  }
  trait ScalaApplication { self: ScalaDependentComponent => }

  // layer 1 - small components
  trait Picture extends ScalaComponent
  trait Stats extends ScalaComponent

  // layer 2 - compose
  trait Profile extends ScalaDependentComponent with Picture
  trait Analytics extends ScalaDependentComponent with Stats

  // layer 3 - app
  trait AnalyticsApp extends ScalaApplication with Analytics
}
