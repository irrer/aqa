/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.web

import org.aqa.Util
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.representation.ByteArrayRepresentation

import java.io.File

class MainIndex(staticDir: File) extends Restlet {

  private val index = {
    val file = new File(staticDir, "index.html")
    new String(Util.readBinaryFile(file).right.get)
  }

  private val favicon = {
    val file = new File(new File(staticDir, "images"), "favicon.ico")
    val bytes = Util.readBinaryFile(file).right.get
    val representation = new ByteArrayRepresentation(bytes, MediaType.IMAGE_ICON, bytes.size)
    representation.setMediaType(MediaType.IMAGE_ICON)
    representation
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    response.setStatus(Status.SUCCESS_OK)

    //    val ref = request.getResourceRef.getRemainingPart.toLowerCase
    //
    //    if (ref.contains("favicon.ico") && (!ref.contains("/static/"))) {
    //      //response.setEntity(favicon) // can not get this working.  Firefox complains:
    //      // The image "http://localhost/favicon.ico" cannot be displayed because it contains errors.
    //
    //      response.redirectSeeOther("/static/images/favicon.ico?")
    //    } else
    response.setEntity(index, MediaType.TEXT_HTML)

    //response.setStatus(Status.SUCCESS_OK)
  }
}
