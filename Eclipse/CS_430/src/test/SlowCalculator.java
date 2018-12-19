package test;

public class SlowCalculator
{
  public int increment(int n)
  {
    lookBusy(5000);
    return n + 1;
  }
  
  private static void lookBusy(long millis)
  {
    long interval = 300;
    long stop = System.currentTimeMillis() + millis;
    
    try
    {
      while(!Thread.currentThread().isInterrupted() && 
             System.currentTimeMillis() < stop)
      {
        System.out.print(".");
        Thread.sleep(interval);      
      }
    }
    catch (InterruptedException ie)
    {}
  }

}