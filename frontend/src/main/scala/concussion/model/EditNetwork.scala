package concussion
package model

import concussion.component.editor.{Connections, EditNode}
import concussion.graph.LGraph

case class EditNetwork(private val rep: LGraph[Connections, EditNode])
