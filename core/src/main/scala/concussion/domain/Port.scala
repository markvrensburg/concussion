package concussion
package domain

final case class Port[Meta](meta: Meta, name: String) {

  def map[B](f: Meta => B): Port[B] =
    this.copy(meta = f(meta))
}
