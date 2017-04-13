package com.cj.serialization

sealed abstract class Result[+A] extends Product with Serializable {

  def fold[B](
               withFailure: String => B,
               withSuccess: A => B
             ): B = this match {
    case RFailure(s) => withFailure(s)
    case RSuccess(v) => withSuccess(v)
  }

  def map[B](f: A => B): Result[B] =
    this.fold(s => Result.failure(s), v => Result.safely(f(v)))

  def flatMap[B](k: A => Result[B]): Result[B] =
    this.fold(s => Result.failure(s), v => k(v))

  def foreach(f: A => Unit): Unit =
    this.fold(_ => (), v => f(v))

  def filter(p: A => Boolean): Result[A] =
    this.fold(
      withFailure = str => Result.failure(str),
      withSuccess = v => p(v) match {
        case false => Result.failure("Failed at filter")
        case true => Result.safely(v)
      }
    )

  def withFilter(p: A => Boolean): Result[A] =
    this.filter(p)

  def failure: Option[String] =
    this.fold(s => Some(s), _ => None)

  def success: Option[A] =
    this.fold(_ => None, v => Some(v))

  def contains[B >: A](b: B): Boolean =
    this.success.contains(b)

  def getOrThrow: A =
    this.fold(msg => throw new Exception(msg), identity)

  def getOrElse[B >: A](b: B): B =
    this.fold(_ => b, identity)

  def isFailure: Boolean =
    this.fold(_ => true, _ => false)

  def isSuccess: Boolean =
    this.fold(_ => false, _ => true)
}

private case class RFailure[T](message: String) extends Result[T]
private case class RSuccess[T](value: T) extends Result[T]

object Result {

  def safely[A](action: => A): Result[A] =
    scala.util.Try(action) match {
      case scala.util.Failure(throwable) => throwable match {
        case _ if throwable.getMessage != null =>
          failure("Result.safely: action threw " ++
            throwable.getClass.getCanonicalName ++ ", " ++ throwable.getMessage)
        case _ =>
          failure(s"Result.safely: action threw ${throwable.getClass.getCanonicalName}")
      }
      case scala.util.Success(value) => value match {
        case _ if value != null =>
          RSuccess(value)
        case _ =>
          failure("Result.safely: action returned null value")
      }
    }

  def failure[A](message: String): Result[A] = message match {
    case _ if message != null => RFailure(message)
    case _ => RFailure("Result.failure: unknown error (error message was null)")
  }

  def ensure(p: Boolean, failMessage: => String): Result[Unit] =
    p match {
      case false => failure(failMessage)
      case true => safely(())
    }

  def fromOption[A](op: Option[A], failMessage: => String): Result[A] =
    op match {
      case None => failure(failMessage)
      case Some(a) => safely(a)
    }

  import scalaz._

  implicit object FunctorResult extends Functor[Result] {
    def map[A, B](fa: Result[A])(f: A => B): Result[B] = fa.map(f)
  }

  implicit object MonadResult extends Monad[Result] {
    def point[A](a: => A): Result[A] = safely(a)
    def bind[A, B](as: Result[A])(k: A => Result[B]): Result[B] = as.flatMap(k)
  }
}
