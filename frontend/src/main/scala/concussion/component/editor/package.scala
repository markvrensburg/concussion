package concussion
package component

import concussion.domain._
import concussion.geometry.Orientation
import japgolly.scalajs.react.Ref.Simple
import org.scalajs.dom.html

package object editor {

  type EditNode =
    Node[Simple[html.Element], (Orientation, Simple[html.Element])]

  type EditPort = Port[(Orientation, Simple[html.Element])]

  type EditNetwork =
    Network[Simple[html.Element], (Orientation, Simple[html.Element]), Connections]
}
