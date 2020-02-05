package learn

import com.google.inject.{ Inject, Singleton }
import play.api.db.slick.DatabaseConfigProvider
import slick.ast.BaseTypedType
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile
import io.strongtyped.active.slick.ActiveRecord
import io.strongtyped.active.slick.EntityActions
//import io.strongtyped.active.slick.ModelIdContract
//
//case class Dog(name: String, id: Option[Long] = None)
//
//@Singleton
//class DogActiveRecord @Inject() (databaseConfigProvider: DatabaseConfigProvider) extends EntityActions {
//
//  override val dbConfig: DatabaseConfig[JdbcProfile] = databaseConfigProvider.get[JdbcProfile]
//
//  import dbConfig.driver.api._
//
//  override def tableQuery = TableQuery(new Dogs(_))
//
//  override def $id(table: Dogs): Rep[Id] = table.id
//
//  override def modelIdContract: ModelIdContract[Dog, Id] = null // ModelIdContract(dog =&gt; dog.id.get, (dog, id) =&gt; dog.copy(id = Some(id)))
//
//  override def baseTypedType: BaseTypedType[Id] = implicitly[BaseTypedType[Id]]
//
//  override type Entity = Dog
//  override type Id = Long
//  override type EntityTable = Dogs
//
//  class Dogs(tag: Tag) extends Table[Dog](tag, "DogsTable") {
//    def name = column[String]("name")
//    def id = column[Long]("id", O.PrimaryKey)
//    def * = (name, id.?) & lt; &gt; (Dog.tupled, Dog.unapply)
//  }
//
//  implicit class ActiveRecordImplicit(val model: Entity) extends ActiveRecord(this)
//
//  import scala.concurrent.ExecutionContext.Implicits.global
//
//  val result = Dog("some_dog").save()
//
//  val res2 = Dog("some_other_dog", Some(1)).delete()
//
//  val res3 = Dog("some_crazy_dog", Some(1)).update()
//}
