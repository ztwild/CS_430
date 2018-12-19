package Presentation;

import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;

public class Sums extends RecursiveTask<Integer>{

	private int[] mSource;
    private int mStart;
    private int mLength;
 
    protected static int sThreshold = 1000;
    
    public Sums(int[] src, int start, int length) {
        mSource = src;
        mStart = start;
        mLength = length;
    }
    
    protected Integer computeDirectly(){
    	int total = 0;
    	for(int i = 0; i < mLength; i++){
    		total += mSource[i];
    	}
    	return total;
    }
	
	
	@Override
	protected Integer compute() {
		if (mLength < sThreshold) {
            
            return computeDirectly();
        }
 
        int split = mLength / 2;
 
        int t1 = new Sums(mSource, mStart, split).invoke();
        int t2 = new Sums(mSource, mStart + split, mLength - split).invoke();
        return t1 + t2;
//        invokeAll(new Sums(mSource, mStart, split),
//                new ForkBlur(mSource, mStart + split, mLength - split));
	}
	
	
	public static void main(String[] args){
		int length = 1000000;
		int should = 0;
		int[] src = new int[length];
		for(int i = 0; i < length; i++){
			src[i] = i;
			should += i;
		}
		
		Sums fb = new Sums(src, 0, src.length);
		 
        ForkJoinPool pool = new ForkJoinPool();
 
        long startTime = System.currentTimeMillis();
        int total = pool.invoke(fb);
        long endTime = System.currentTimeMillis();
 
        System.out.println("Sums took " + (endTime - startTime) + 
                " milliseconds.");
        System.out.println("Total = "+total);
        System.out.println("should = "+should);
		
	}

	
}
