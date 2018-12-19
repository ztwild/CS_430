package hw1;

import java.awt.Point;

public class ImmutableTrajectory
{
  private final Point[] data;

  public ImmutableTrajectory(Point[] data)
  {
  	this.data = copy(data);
  }
  
  public Point[] getValues()
  {
    return copy(data);
  }
  
  public Point getValue(int index)
  {
  	return copy(data[index]);
  }
  
  /** Used to prevent aliasing between this class's state and its clients. */
  private static Point[] copy(Point[] orig) {
    Point[] copy = new Point[orig.length];
    for (int idx = 0; idx < orig.length; idx++) {
      copy[idx] = copy(orig[idx]);
    }
    return copy;
  }

  private static Point copy(Point p) {
    // Notice that we don't want to use `Object#clone()` here. `p` could be an
    // instance of some crazy subtype of `Point` whose `clone()` method creates
    // aliases between mutable state. By calling the constructor of `Point`, it
    // won't matter if our `p` is a crazy subtype. We can look at the code for
    // `Point` and see that it will just copy over the (primitive) values of
    // `x` and `y` into a new `Point` instance. By using the constructor, we know
    // that this context will completely "own" the state of our copy.
    return new Point(p);
  }
  
}
