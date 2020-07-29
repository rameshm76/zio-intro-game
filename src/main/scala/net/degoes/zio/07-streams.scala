package net.degoes.zio
import zio._
object ConsoleInput extends App {
  import java.io.IOException

  import zio.console._
  import zio.stream._

  /**
   * EXERCISE
   *
   * Using `ZStream.fromEffect` and `getStrLn`, construct a stream that
   * will emit a single string, taken from the console.
   */
  val singleRead: ZStream[Console, IOException, String] = ZStream.fromEffect(getStrLn)

  /**
   * Using `ZStream#forever`, take the `singleRead` stream, and turn it into
   * a stream that repeats forever.
   */
  val consoleInput: ZStream[Console, IOException, String] = singleRead.forever

  sealed trait Command
  object Command {
    case object Quit                    extends Command
    case class Unknown(command: String) extends Command
  }

  val commandInput = consoleInput.map[Command] {
    case "quit" => Command.Quit
    case x      => Command.Unknown(x)
  }

  def run(args: List[String]) =
    commandInput
      .tap(command => putStrLn(s"You entered: ${command}"))
      .takeUntil(_ == Command.Quit)
      .runDrain
      .ignore as ExitCode.success

}
