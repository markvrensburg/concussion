package concussion
package component
package editor

import concussion.geometry.{Anchor, Point}

final case class PortMeta(id: String, anchor: Anchor)
final case class NodeMeta(id: String, point: Point)
