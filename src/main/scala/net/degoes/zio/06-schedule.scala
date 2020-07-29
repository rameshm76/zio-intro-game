package net.degoes.zio

import zio._

object Retry extends App {

  /**
   * EXERCISE
   *
   * Using `Schedule.recurs`, create a schedule that recurs 5 times.
   */
  val fiveTimes: Schedule[Any, Any, Int] = Schedule.recurs(4)

  /**
   * EXERCISE
   *
   * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
   * console.
   */
  val repeated1 = ZIO.effectTotal(println("Hello World")).repeat(fiveTimes)

  /**
   * EXERCISE
   *
   * Using `Schedule.spaced`, create a schedule that recurs forever every 1 second.
   */
  import zio.duration._
  val everySecond = Schedule.spaced(1.second)

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
   * and the `everySecond` schedule, create a schedule that repeats fives times,
   * evey second.
   */
  val fiveTimesEverySecond = everySecond && fiveTimes

  /**
   * EXERCISE
   *
   * Using the `ZIO#repeat`, repeat the action putStrLn("Hi hi") using
   * `fiveTimesEverySecond`.
   */
  import zio.console._
  val repeated2 = putStrLn("Hi hi").repeat(fiveTimesEverySecond)

  /**
   * EXERCISE
   *
   * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats fives times rapidly, and then
   * repeats every second forever.
   */
  val fiveTimesThenEverySecond = fiveTimes andThen everySecond

  /**
   * EXERCISE
   *
   * Using `ZIO#retry`, retry the following error a total of five times.
   */
  val error1   = IO.fail("Uh oh!")
  val retried5 = error1 retry fiveTimes

  /**
   * EXERCISE
   *
   * Using the `Schedule#||`, the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats the minimum of five times and
   * every second.
   */
  val fiveTimesOrEverySecond = fiveTimes || everySecond

  /**
   * EXERCISE
   *
   * Using `Schedule.exponential`, create an exponential schedule that starts
   * from 10 milliseconds.
   */
  val exponentialSchedule = Schedule.exponential(10.millisecond)

  /**
   * EXERCISE
   *
   * Using `Schedule.jittered` produced a jittered version of `exponentialSchedule`.
   */
  val jitteredExponential = exponentialSchedule.jittered

  /**
   * EXERCISE
   *
   * Using `Schedule.whileOutput`, produce a filtered schedule from `Schedule.forever`
   * that will halt when the number of recurrences exceeds 100.
   */
  val oneHundred = Schedule.forever
    .whileOutput(_ <= 99)
    .onDecision((a: Unit, s) =>
      s match {
        case None      => console.putStrLn(s"done repeating")
        case Some(att) => console.putStrLn(s"$att")
      }
    )

  /**
   * EXERCISE
   *
   * Using `Schedule.identity`, produce a schedule that recurs forever, without delay,
   * returning its inputs.
   */
  def inputs[A]: Schedule[Any, A, A] = Schedule.identity[A]

  /**
   * EXERCISE
   *
   * Using `Schedule#collect`, produce a schedule that recurs forever, collecting its
   * inputs into a list.
   */
  def collectedInputs[A]: Schedule[Any, A, List[A]] = Schedule.collectAll.whileOutput(_.length < 5)

  /**
   * EXERCISE
   *
   * Using  `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
   * the output of `everySecond`.
   */
  val fiveTimesEverySecondR = fiveTimes *> everySecond

  /**
   * EXERCISE
   *
   * Produce a jittered schedule that first does exponential spacing (starting
   * from 10 milliseconds), but then after the spacing reaches 60 seconds,
   * switches over to fixed spacing of 60 seconds between recurrences, but will
   * only do that for up to 100 times, and produce a list of the inputs to
   * the schedule.
   */
  import zio.random.Random
  import Schedule.{ collectAll, exponential, fixed, recurs }
  def mySchedule[A]: Schedule[ZEnv, A, List[A]] =
    (Schedule
      .exponential(10.millisecond)
      .jittered
      .whileOutput(_ < 30.seconds) andThen (Schedule.spaced(60.second) && Schedule.recurs(100))) *> Schedule.collectAll

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      lst <- ZIO.succeed(s"${new java.util.Date}").repeat(mySchedule)
      _   <- putStrLn(lst.mkString("\n"))
    } yield ()).exitCode

}
