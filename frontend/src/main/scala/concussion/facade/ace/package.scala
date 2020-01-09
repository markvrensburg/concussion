package concussion
package facade

import scala.scalajs.js

package ace {

  @js.native
  trait EditorProps extends js.Object {
    var $blockScrolling: js.UndefOr[Boolean]
  }

  object EditorProps {

    def apply(blockScrolling: js.UndefOr[Boolean] = js.undefined): EditorProps = {
      val p = (new js.Object).asInstanceOf[EditorProps]
      p.$blockScrolling = blockScrolling
      p
    }
  }

  private[ace] object rawjs {
    type RawOnChange = js.Function1[String, Unit]
  }

}
