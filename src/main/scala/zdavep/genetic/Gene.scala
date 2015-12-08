package zdavep.genetic

trait Gene { self =>
  def copy: Gene = self
}
