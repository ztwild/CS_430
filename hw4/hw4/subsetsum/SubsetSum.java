package hw4.subsetsum;

import java.util.ArrayList;
import java.util.List;

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
  public static List<List<Integer>> findSubsetSumFJ(List<Integer> set, int target)
  {
    // TODO - implement this using Fork-Join
    return null;
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
