package hw3;

import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RecursiveAction;
import java.util.concurrent.RecursiveTask;


/**
 * Creates a histogram of values produced by Java's random
 * number generator.
 */
public class Histogram3 extends RecursiveAction
{  
  public static void main(String[] args)
  {
    new Histogram3(10000, 100000000).go();
  }
  
  private int maxValue;
  private int numSamples;
  private int[] results;
  private ForkJoinTask task1, task2;
  
  public Histogram3(int givenMax, int givenNum)
  {
    maxValue = givenMax;
    numSamples = givenNum;
    results = new int[maxValue];
  }
  
  public void go()
  {
	  ForkJoinPool pool = new ForkJoinPool();
	
    long start = System.currentTimeMillis();
    pool.invoke(new Histogram3(maxValue, numSamples));
//    pool.execute(new Histogram3(maxValue, numSamples));
    long elapsed = System.currentTimeMillis() - start;

    int total = 0;
    for (int i = 0; i < results.length; ++i)
    {
      total += results[i];
      System.out.println(i + ": " + results[i]);
    }
    System.out.println();
    System.out.println("Check total samples: " + total);
    System.out.println("Elapsed: " + elapsed);
    System.out.println("Histogram3");

  }
  
  int sThreshold = 100;
  
  @Override
  protected void compute() {
      if (numSamples < sThreshold) {
          computeDirectly();
          return;
      }

      int split = numSamples / 2;
      
//      new Histogram3(maxValue, split).fork();
//      new Histogram3(maxValue, numSamples - split).fork();
      invokeAll(new Histogram3(maxValue, split), new Histogram3(maxValue, numSamples - split));
  }
  
  protected void computeDirectly() {
	  Random rand = new Random();
    for (int i = 0; i < numSamples; ++i)
    {
      int next = rand.nextInt(maxValue);
      results[next]++;
    }
    
  }
  
  
  
}
