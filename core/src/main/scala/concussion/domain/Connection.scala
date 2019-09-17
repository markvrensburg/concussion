package concussion
package domain

final case class Connection[A](from: A, to: A)
