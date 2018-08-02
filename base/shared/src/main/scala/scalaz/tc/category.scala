package scalaz
package tc

trait CategoryClass[=>:[_, _]] extends SemicategoryClass[=>:] {
  def id[A]: A =>: A
}
