package scalaz
package ct

trait CategoryClass[=>:[_, _]] extends SemicategoryClass[=>:] {
  def id[A]: A =>: A
}
