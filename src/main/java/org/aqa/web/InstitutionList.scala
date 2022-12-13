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

import org.aqa.db.Institution
import org.aqa.web.WebUtil._
import org.aqa.AnonymizeUtil
import org.restlet.Response

import scala.xml.Elem

object InstitutionList {
  private val path = new String((new InstitutionList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class InstitutionList extends GenericList[Institution] with WebUtil.SubUrlAdmin {

  private def humanReadableURL(url: String): String = {
    val small = url.replaceAll("^https://", "").replaceAll("^http://", "").replaceAll("^www\\.", "")
    WebUtil.firstPartOf(small, 20)
  }

  private def urlHTML(institution: Institution): Elem = {
    val url = institution.url_real.trim
    if (url.size > 0)
      <a href={url} target="_blank" title={url}>{humanReadableURL(url)}</a>
    else <div></div>
  }

  private def descriptionHTML(institution: Institution): Elem = {
    <div> {WebUtil.firstPartOf(institution.description_real, 60)} </div>
  }

  private val idCol = new Column[Institution]("Name", _.name, (inst) => makePrimaryKeyHtmlWithAQAAlias(inst.name, inst.institutionPK))

  private val urlCol = encryptedColumn("URL", AnonymizeUtil.institutionAliasUrlPrefixId, (inst) => inst.institutionPK.get)

  new Column[Institution]("URL", _.url_real, urlHTML)

  private val descriptionCol = encryptedColumn("Description", AnonymizeUtil.institutionAliasDescriptionPrefixId, (inst) => inst.institutionPK.get)

  override def listName = "Institution"

  override val columnList = Seq(idCol, urlCol, descriptionCol)

  override def getData(valueMap: ValueMapT, response: Response) = Institution.list

  override def getPK(value: Institution): Long = value.institutionPK.get
}
