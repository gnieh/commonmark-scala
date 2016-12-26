/* Copyright (c) 2016 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package object commonmark {

  import ast._

  implicit class InlinesOps(val inlines: Seq[Inline]) extends AnyVal {

    def append(i: Inline): Seq[Inline] =
      (i, inlines.lastOption) match {
        case (Text(t1), Some(Text(t2))) =>
          inlines.init :+ Text(t1 ++ t2)
        case (_, _) =>
          inlines :+ i
      }

  }

}
