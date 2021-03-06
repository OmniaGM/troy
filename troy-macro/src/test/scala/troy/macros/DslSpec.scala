/*
 * Copyright 2016 Tamer AbdulRadi
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

package troy.macros

import java.util.UUID

import com.datastax.driver.core._

import scala.concurrent.Future

class DslSpec extends CassandraSpec {

  import troy.driver.DSL._
  import troy.dsl._
  import scala.concurrent.ExecutionContext.Implicits.global

  override val testDataFixtures =
    """
      INSERT INTO test.posts (author_id, post_id , author_name , post_rating, post_title)
      VALUES ( uuid(), uuid(), 'test author', 5, 'test post') ;

      INSERT INTO test.post_details (author_id, id , tags , comment_ids, comment_userIds, comment_bodies , comments)
      VALUES ( uuid(), uuid(), {'test1', 'test2'}, {1, 2}, [1, 2], ['test1', 'test2'], {1: 'test1', 2 : 'test2'}) ;
    """

  case class Post(id: UUID, author_name: String, title: String)

  "The Macro" should "support no params" in {
    val q = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared.executeAsync.all.as(Post)
    }
    q(): Future[Seq[Post]]
  }

  it should "support prepare & execute async" in {
    val q = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared.executeAsync.all.as(Post)
    }
    q(): Future[Seq[Post]]
  }

  it should "support single param" in {
    val q = withSchema { (title: String) =>
      cql"SELECT post_id, author_name, post_title FROM test.posts WHERE post_title = $title;".prepared.executeAsync.all.as(Post)
    }
    q("test"): Future[Seq[Post]]
  }

  "The Macro" should "support returning the BoundStatement directly with no params" in {
    val q = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared
    }
    q(): Statement
  }

  "The Macro" should "support returning the BoundStatement directly with params" in {
    val q = withSchema { (title: String) =>
      cql"SELECT post_id, author_name, post_title FROM test.posts WHERE post_title = $title;".prepared
    }
    q("test"): Statement
  }

  it should "support returning the ResultSet" in {
    val query = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared.execute
    }
    val result: ResultSet = query()
    result.all().size() shouldBe 1
  }

  it should "support returning the ResultSet asynchronously" in {
    val q = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared.executeAsync
    }
    q(): Future[ResultSet]
  }

  it should "support returning one element" in {
    val q = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared.executeAsync.oneOption
    }
    q(): Future[Option[Row]]
  }
  it should "allow specifying consistency level" in {
    withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;"
        .prepared
        .setConsistencyLevel(ConsistencyLevel.LOCAL_QUORUM)
        .setSerialConsistencyLevel(ConsistencyLevel.LOCAL_SERIAL)
        .executeAsync
        .all
        .as(Post)
    }
  }

  it should "support parsing one row async" in {
    val q = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared.executeAsync.oneOption.as(Post)
    }
    q(): Future[Option[Post]]
  }

  it should "support parsing one row sync" in {
    val q = withSchema { () =>
      cql"SELECT post_id, author_name, post_title FROM test.posts;".prepared.execute.oneOption.as(Post)
    }
    q(): Option[Post]
  }

  it should "support select * with no params" in {
    val q = withSchema { () =>
      cql"SELECT * FROM test.posts;".prepared.execute.oneOption
    }
    q(): Option[Row]
  }

  it should "support select with orderby" in {
    val listByAuthor = withSchema { (authorId: UUID) =>
      cql"""
         SELECT author_id, author_name, post_title
         FROM test.posts
         WHERE author_id = $authorId
         ORDER BY post_id DESC ;
       """
        .prepared
        .executeAsync
        .all
        .as(Post)
    }
    listByAuthor(UUID.randomUUID()): Future[Seq[Post]]
  }

  //    TODO https://github.com/tabdulradi/troy/issues/37
  //  it should "support parsing select * with class/function matching the whole table" in {
  //    val q = withSchema { () =>
  //      cql"SELECT * FROM test.posts;".prepared.execute.all.as(AuthorAndPost)
  //    }
  //    val res: Seq[AuthorAndPost] = q()
  //  }

  it should "support specifying minimum version" in {
    val q = withSchema.minVersion(1) { () =>
      cql"SELECT author_id FROM test.posts;".prepared.execute.oneOption
    }
    q(): Option[Row]
  }

  it should "support specifying minimum version and maximum version" in {
    val q = withSchema.minVersion(1).maxVersion(2) { () =>
      cql"SELECT author_id FROM test.posts;".prepared.execute.oneOption
    }
    q(): Option[Row]
  }
}
