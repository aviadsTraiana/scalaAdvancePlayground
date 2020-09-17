trait File{
  def open():Unit
}
trait InputFile extends File {
    override def open():Unit = println("opened file as input")
    def read():String = "some content"
}

trait OutputFile extends InputFile {
    override def open():Unit = println("opened file as output")
    def write(content:String):Unit = println(s"writing to file $content")
}
class InputOutputFile  extends OutputFile {
    override def open(): Unit = {
      println("what is going to be opened here?")
      super.open()
    }
}
new InputOutputFile().open()

