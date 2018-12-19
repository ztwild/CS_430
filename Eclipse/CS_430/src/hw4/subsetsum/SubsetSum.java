package hw4.subsetsum;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveAction;
import java.util.concurrent.RecursiveTask;

import static java.util.Collections.emptyList;

public class SubsetSum 
{
  
  /**
   * Given a nonempty set of integers, finds all subsets whose sum is the given
   * target value.  Returns a list (possibly empty) of lists of values from 
   * the given set.
   * @param set
   * @param target
   * @return
   */

  
  private static class SubsetSumTask extends RecursiveTask<List<List<Integer>>>{
    private List<Integer> set;
    private int target;
    
    
    public SubsetSumTask(List<Integer> set, int target){
      this.set = set;
      this.target = target;
      
    }
    
    @Override
    public List<List<Integer>> compute(){
      List<List<Integer>> toReturn = new ArrayList<>();
      if(set.size() == 1){
        if (set.get(0).equals(target))
        {
          List<Integer> result = new ArrayList<>();
          result.add(target);
          toReturn.add(result);
        }
      }
      else{
        ArrayList<Integer> temp = new ArrayList<>();
        temp.addAll(set);
        int lastValue = temp.remove(temp.size() - 1); // remove at index i
        
        SubsetSumTask taskWithout = new SubsetSumTask(temp, target);
        SubsetSumTask taskWith = new SubsetSumTask(temp, target - lastValue);
        
        taskWithout.fork();
        
        List<List<Integer>> resultsWithLastValue = taskWith.compute();
        if (resultsWithLastValue.size() != 0)
        {
          for (List<Integer> lst : resultsWithLastValue)
          {
            lst.add(lastValue);
          }
          toReturn.addAll(resultsWithLastValue);
        }
        else
        {
          if (lastValue == target)
          {
            List<Integer> result = new ArrayList<>();
            result.add(target);
            toReturn.add(result);
          }
        }
        
        List<List<Integer>> resultsWithoutLastValue = taskWithout.join();
        if (resultsWithoutLastValue.size() != 0)
        {
          toReturn.addAll(resultsWithoutLastValue);
        }
        
      }
      return toReturn;
    }
    
  }
  
  
  public static List<List<Integer>> findSubsetSumFJ(List<Integer> set, int target)
  {
    ForkJoinPool pool = new ForkJoinPool();
    SubsetSumTask task = new SubsetSumTask(set, target);
    pool.invoke(task);
    return task.join();
  }
  
  
  /**
   * Given a nonempty set of integers, finds all subsets whose sum is the given
   * target value.  Returns a list (possibly empty) of lists of values from 
   * the given set.
   * @param set
   * @param target
   * @return
   */
  public static List<List<Integer>> findSubsetSum(List<Integer> set, int target)
  {
    List<List<Integer>> toReturn = new ArrayList<>();
    
    if (set.size() == 1)
    {
      // base case
      if (set.get(0).equals(target))
      {
        List<Integer> result = new ArrayList<>();
        result.add(target);
        toReturn.add(result);
      }
    }
    else
    {
      // create copy of set, but with one value removed
      ArrayList<Integer> temp = new ArrayList<>();
      temp.addAll(set);
      int lastValue = temp.remove(temp.size() - 1); // remove at index i

      // try finding subsets that add up to target, without the last value
      List<List<Integer>> resultsWithoutLastValue = findSubsetSum(temp, target);
      if (resultsWithoutLastValue.size() != 0)
      {
        toReturn.addAll(resultsWithoutLastValue);
      }
      
      // try finding subsets that add up to target, if last value is added too
      List<List<Integer>> resultsWithLastValue = findSubsetSum(temp, target - lastValue);
      if (resultsWithLastValue.size() != 0)
      {
        // put the missing value back into the solution
        for (List<Integer> lst : resultsWithLastValue)
        {
          lst.add(lastValue);
        }
        toReturn.addAll(resultsWithLastValue);
      }
      else
      {
        // no results, but the one element set with just 'lastValue' 
        // could be a solution too
        if (lastValue == target)
        {
          List<Integer> result = new ArrayList<>();
          result.add(target);
          toReturn.add(result);
        }
      }
    }    
    return toReturn;
  }
}
