package org.aqac.db

import slick.driver.PostgresDriver.api._
import org.aqac.Logging._
import org.aqac.Config
import org.aqac.Util

case class MaxLeafGap(
        maxLeafGapPK: Option[Long], // primary key
        outputPK: Long, // company name
        maxLeafGap: Double // maximum leaf gap
        ) {

    def insert: MaxLeafGap = {
        val insertQuery = MaxLeafGap.query returning MaxLeafGap.query.map(_.maxLeafGapPK) into ((maxLeafGap, maxLeafGapPK) => maxLeafGap.copy(maxLeafGapPK = Some(maxLeafGapPK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(MaxLeafGap.query.insertOrUpdate(this))

    override def toString: String = (maxLeafGap.toString).trim
}

object MaxLeafGap {
    class MaxLeafGapTable(tag: Tag) extends Table[MaxLeafGap](tag, "maxLeafGap") {

        def maxLeafGapPK = column[Long]("maxLeafGapPK", O.PrimaryKey, O.AutoInc)
        def outputPK = column[Long]("outputPK")
        def maxLeafGap = column[Double]("maxLeafGap")
        def status = column[String]("status")

        def * = (
            maxLeafGapPK.?,
            outputPK,
            maxLeafGap) <> ((MaxLeafGap.apply _)tupled, MaxLeafGap.unapply _)
    }

    val query = TableQuery[MaxLeafGapTable]

    def get(maxLeafGapPK: Long): Option[MaxLeafGap] = {
        val action = for {
            inst <- MaxLeafGap.query if inst.maxLeafGapPK === maxLeafGapPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all maxLeafGaps.
     */
    def list = Db.run(query.result)

    def delete(maxLeafGapPK: Long): Int = {
        val q = query.filter(_.maxLeafGapPK === maxLeafGapPK)
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
            output <- Output.query if output.inputPK === input.inputPK
            maxLeafGap <- MaxLeafGap.query if (maxLeafGap.outputPK === output.outputPK)
        } yield (input.dataDate, maxLeafGap.maxLeafGap)

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
