package munit

class FailException(
    val message: String | Null,
    val cause: Throwable | Null,
    val isStackTracesEnabled: Boolean,
    val location: Location
) extends AssertionError(message, cause)
    with FailExceptionLike[FailException]
    with Serializable {
  def this(message: String, location: Location) =
    this(message, null, true, location)
  def this(message: String, cause: Throwable, location: Location) = this(
    message,
    cause,
    true,
    location
  )
  def withMessage(newMessage: String): FailException =
    copy(message = newMessage)
  def copy(
      message: String | Null = this.message,
      cause: Throwable | Null = this.cause,
      isStackTracesEnabled: Boolean = this.isStackTracesEnabled,
      location: Location = this.location
  ): FailException =
    new FailException(message, cause, isStackTracesEnabled, location)

  override def fillInStackTrace(): Throwable = {
    val result = super.fillInStackTrace()
    if (!isStackTracesEnabled) {
      result.setStackTrace(result.getStackTrace().slice(0, 1))
    }
    result
  }
}
