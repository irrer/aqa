package org.aqac.db

import slick.driver.PostgresDriver.api._
import org.aqac.Logging._
import org.aqac.Config
import org.aqac.Util

case class CentralAxis(
        centralAxisPK: Option[Long], // primary key
        outputPK: Long, // company name
        centralAxis: Double // maximum leaf gap
        ) {

    def insert: CentralAxis = {
        val insertQuery = CentralAxis.query returning CentralAxis.query.map(_.centralAxisPK) into ((centralAxis, centralAxisPK) => centralAxis.copy(centralAxisPK = Some(centralAxisPK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(CentralAxis.query.insertOrUpdate(this))

    override def toString: String = (centralAxis.toString).trim
}

object CentralAxis {
    class CentralAxisTable(tag: Tag) extends Table[CentralAxis](tag, "centralAxis") {

        def centralAxisPK = column[Long]("centralAxisPK", O.PrimaryKey, O.AutoInc)
        def outputPK = column[Long]("outputPK")
        def centralAxis = column[Double]("centralAxis")
        def status = column[String]("status")

        def * = (
            centralAxisPK.?,
            outputPK,
            centralAxis) <> ((CentralAxis.apply _)tupled, CentralAxis.unapply _)
    }

    val query = TableQuery[CentralAxisTable]

    def get(centralAxisPK: Long): Option[CentralAxis] = {
        val action = for {
            inst <- CentralAxis.query if inst.centralAxisPK === centralAxisPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all centralAxiss.
     */
    def list = Db.run(query.result)

    def delete(centralAxisPK: Long): Int = {
        val q = query.filter(_.centralAxisPK === centralAxisPK)
        val action = q.delete
        Db.run(action)
    }
    
    def deleteByOutputPK(outputPK: Long): Int = {
        val q = query.filter(_.outputPK === outputPK)
        val action = q.delete
        Db.run(action)
    }
    
    

    def getHistory(machinePK: Long, maxSize: Int): Seq[Double] = {
        val search = for {
            machine <- Machine.query if machine.machinePK === machinePK
            input <- Input.query if input.machinePK === machinePK
            output <- Output.query if (output.inputPK === input.inputPK) && (output.dataValidity === DataValidity.valid.toString)
            centralAxis <- CentralAxis.query if (centralAxis.outputPK === output.outputPK)
        } yield (input.dataDate, centralAxis.centralAxis)

        val sorted = search.sortBy(_._1.desc.reverse).take(maxSize)

        val list = Db.run(sorted.result)
        list.map(tv => println(tv._1, tv._2))
        list.map(tv => tv._2)
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        getHistory(2, 10)
        //println("======== inst: " + get(5))
        //println("======== inst delete: " + delete(5))
    }
}
