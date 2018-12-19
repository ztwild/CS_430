package hw3;

import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RecursiveAction;
import java.util.concurrent.RecursiveTask;


/**
 * Creates a histogram of values produced by Java's random
 * number generator.
 */
public class Histogram2 extends RecursiveTask
{  
  public static void main(String[] args)
  {
    new Histogram2(10000, 100000000).go();
  }
  
  private int maxValue;
  private int numSamples;
  private int[] results;
  private ForkJoinTask task1, task2;
  
  public Histogram2(int givenMax, int givenNum)
  {
    maxValue = givenMax;
    numSamples = givenNum;
    results = new int[maxValue];
  }
  
  public void go()
  {
	  ForkJoinPool pool = new ForkJoinPool();
	  Histogram2 task = new Histogram2(maxValue, numSamples);
	
    long start = System.currentTimeMillis();
    pool.execute(task);
    FutureTask<int[]> future = (FutureTask<int[]>) task.join();
    long elapsed = System.currentTimeMillis() - start;
    System.out.println("Elapsed: " + elapsed);
    
    int total = 0;
    try {
			results = future.get();
			for (int i = 0; i < results.length; ++i)
	    {
	      total += results[i];
	      //System.out.println(i + ": " + results[i]);
	    }
		} 
    catch (InterruptedException e) { e.printStackTrace(); } 
    catch (ExecutionException e) 	 { e.printStackTrace(); }
    
    System.out.println();
    System.out.println("Check total samples: " + total);
    System.out.println("Elapsed: " + elapsed);
    System.out.println("Histogram2");

  }
  
  int sThreshold = 100;
  
  @Override
  protected Future<int[]> compute() {
      if (numSamples < sThreshold) {
          return computeDirectly();
      }
      int split = numSamples / 2;
      
      Histogram2 t1 = new Histogram2(maxValue, split);
      Histogram2 t2 = new Histogram2(maxValue, numSamples - split);
      invokeAll(t1, t2);
      
      Callable<int[]> getResults = new Callable<int[]>(){
      	public int[] call() throws InterruptedException{
					try {
						System.out.println("numSamples = "+numSamples);
						int arr1[] = (int[]) t1.get();
						int arr2[] = (int[]) t2.get();
	      		for(int i = 0; i < maxValue; i++){
	      			results[i] = arr1[i] + arr2[i];
	      		}
	      		
	      		return results;
					} 
					catch (ExecutionException e) { e.printStackTrace(); return null; }
      		
      	}
      };
      return new FutureTask<int[]>(getResults);
  }
  
  protected Future<int[]> computeDirectly() {
	  Random rand = new Random();
    for (int i = 0; i < numSamples; ++i)
    {
      int next = rand.nextInt(maxValue);
      results[next]++;
    }
    
  	Callable<int[]> getResults = new Callable<int[]>(){
			public int[] call() throws InterruptedException{
				System.out.println("numSamples = "+numSamples);
				return results;
			}
		};
			
    return new FutureTask<int[]>(getResults);
  }
  
  
  
}
