package concussion
package component
package editor

import concussion.domain.Orientation

final case class Anchor(x: Double, y: Double, orientation: Orientation)

final case class PortId(id: String, nodeId: String)

final case class Port(id: PortId, anchor: Anchor)
