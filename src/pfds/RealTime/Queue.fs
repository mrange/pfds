// ----------------------------------------------------------------------------------------------
// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------
[<RequireQualifiedAccess>]
module pfds.RealTime.Queue

open pfds.Common

type Type<'T> = Stream<'T>*'T list*Stream<'T>

// [<GeneralizableValue>]
let inline empty () = Stream.empty (), [], Stream.empty ()

let inline isEmpty (q : Type<'T>) : bool =
  match q with
  | (Stream.Empty, _, _) -> true
  | _ -> false

let rec internal rotate (q : Type<'T>) =
  match q with
  | (Stream.Empty, y::_, a)         -> Stream.cons y a
  | (Stream.Cons (x,xs), y::ys, a)  ->
    Stream.cons x (rotate (xs, ys, Stream.cons y a))
