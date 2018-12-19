package hw3;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RecursiveAction;
import java.util.concurrent.RecursiveTask;


/**
 * Creates a Histogram of values produced by Java's random
 * number generator.
 */
public class Histogram implements Callable<int[]>
{  
  public static void main(String[] args)
  {
    new Histogram(10000, 100000000).go();
    										// 25000000
  }
  
  private int maxValue;
  private int numSamples;
  private int[] results;
  
  public Histogram(int givenMax, int givenNum)
  {
    maxValue = givenMax;
    numSamples = givenNum;
    results = new int[maxValue];
  }
  
  @Override
  public int[] call() {
  	Random rand = new Random();
    for (int i = 0; i < numSamples; ++i)
    {
      int next = rand.nextInt(maxValue);
      results[next]++;
    }
    
    return results;
  }    
  
  public void go()
  {
  	int cores = Runtime.getRuntime().availableProcessors();
  	ExecutorService pool = Executors.newFixedThreadPool(cores);
  	int totalSamples = 0;
  	
  	List<Histogram> list = new ArrayList<Histogram>();
  	for(int i = 0; i < cores; i++){
  		totalSamples += numSamples/cores;
  		list.add(new Histogram(maxValue, numSamples/cores));
  	}
  	if(totalSamples < numSamples) list.add(new Histogram(maxValue, numSamples - totalSamples));
  	
    long start = System.currentTimeMillis();
    List<Future<int[]>> elements = null;
    try {
			elements = pool.invokeAll(list);
		} catch (InterruptedException e) { e.printStackTrace(); }
    pool.shutdown();
    
    long elapsed = System.currentTimeMillis() - start;
    
    
    for(Future<int[]> element : elements){
    	int arr[];
			try {
				arr = element.get();
				for(int i = 0; i < maxValue; i++){
	    		results[i] += arr[i];
	    	}
			}
			catch (InterruptedException e) { e.printStackTrace(); } 
			catch (ExecutionException e) { e.printStackTrace(); }
    	
    }
    
    int total = 0;
    for (int i = 0; i < results.length; ++i)
    {
      total += results[i];
      System.out.println(i + ": " + results[i]);
    }
    System.out.println();
    System.out.println("Check total samples: " + total);
    System.out.println("Elapsed: " + elapsed);

  }  
  
}
