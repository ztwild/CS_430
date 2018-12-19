package test;

import java.util.ArrayList;
import java.util.List;

public class ChallengeOne {
  
  
  public int SumOfDigits(int num){
    int total = 0, base = 10;
    while(num > 10){
      total += num % base;
      num /= 10;
    }
    total += num;
    return total;
  }
  
  public int SumOfZero(int[] list){
    int total = 0;
    
    List<Integer> total1 = SumOfZeroRec(list, 1);
    List<Integer> total2 = new ArrayList<Integer>(total1);
//    total2.stream().forEach(x -> x += list[0]);
    for(int i = 0; i < total2.size(); i++){
      int num = total2.get(i) + list[0];
      total2.set(i, num);
    }
    total2.add(list[0]);
    total2.addAll(total1);
    for(int i = 0; i < total2.size(); i++){
      if(total2.get(i) == 0) total++;
    }
    return total;
  }
  
  public List<Integer> SumOfZeroRec(int[] list, int start){
    if(start >= list.length){ return new ArrayList<Integer>(); }
    List<Integer> total1 = SumOfZeroRec(list, start + 1);
    List<Integer> total2 = new ArrayList<Integer>(total1);
//    total2.stream().forEach(x -> x += list[start]);
    for(int i = 0; i < total2.size(); i++){
      int num = total2.get(i) + list[start];
      total2.set(i, num);
    }
    total2.add(list[start]);
    total2.addAll(total1);
    
    return total2;
  }
}
