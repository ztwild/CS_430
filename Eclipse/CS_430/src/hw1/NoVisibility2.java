package hw1;

/**
 * I think this class properly uses synchronization locks because number is 
 * shared between both threads and releases the lock for the other 
 * thread to read the updated value.  This follows rules 1 & 2 of 
 * Synchronization.
 */
public class NoVisibility2
{
  private int number;
  private int count;
  private final int ITERATIONS = 10000;
  
  public static void main(String[] args)
  {
    NoVisibility2 nv = new NoVisibility2();
    nv.go(); 
  }
  
  private void go()
  {
    new ReaderThread().start();
    
    System.out.println("Main thread starting loop");

    for (int i = 0; i <= ITERATIONS; ++i)
    {
      synchronized(this)
      {
        number = i;
      }
      Thread.yield();
    }
  }
  
  class ReaderThread extends Thread
  {
    
    public int get()
    {
      while (true)
      {
        ++count;
        synchronized(NoVisibility2.this)
        {
          if (number >= ITERATIONS) break;
        }
      }
      return number;
    }
    
    public void run()
    {
      System.out.println("Reader starting...");
      int result = get();
      System.out.println("Reader sees number: " + result);
    }
  }
}

