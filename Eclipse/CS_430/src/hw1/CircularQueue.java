package hw1;

import java.util.NoSuchElementException;

/**
 * A standard singly-linked circular FIFO queue with tail pointer.
 * For an empty queue, the tail pointer is null; for a nonempty
 * queue, the head of the queue is tail.next.  (When the queue has 
 * one element, tail.next points to tail.)
 *
 * 4a)  Thread A: Get Node tail
 * 		Thread B: Get Node tail
 * 		Thread B: Copy tail
 * 		Thread B: Adds next Node to copy
 * 		Thread A: Copy tail
 * 		Thread A: Adds next Node to copy
 * 		Thread B: Writes copy to tail
 * 		Thread A: Writes copy to tail
 * 
 * 		Due to context switching between threads when using the same
 * 		variable when they are both trying to add, Threads A or B could
 * 		be overwritten by the other when they are not properly synchronized
 *
 * 4b)	Without synchronization, the variable of tail
 * 		is not guaranteed to follow the 'happens-before'
 * 		relationship
 * 		
 * 		Thread A : 
 * 			tail.add(node_A);
 * 			tail.add(node_B);
 * 			tail.add(node_C);
 * 		
 * 		Thread B :
 * 			tail == node_A;
 * 			tail.next == node_B;
 * 			tail.next.next == node_C;
 * 		
 * 		But without the sychronized attribute of add() and remove()
 * 		Thread A could add node_A node_C and Thread B could return true, false, then nullPointer
 */
public class CircularQueue<T>
{
  private Node tail = null;
  
  public void add(T item)
  {
    Node temp = new Node(item);
    if (tail == null)
    {
      // empty
      temp.next = temp;
    }
    else
    {
      temp.next = tail.next;
      tail.next = temp;
    }
    tail = temp;
  }
  
  public T remove()
  {
    if (tail == null) throw new NoSuchElementException();
    T ret = tail.next.data;
    if (tail.next == tail)
    {
      tail = null;
    }
    else
    {
      tail.next = tail.next.next;
    }
    
    return ret;
  }
  
  public boolean isEmpty()
  {
    return tail == null;
  }
  
  class Node
  {
    T data;
    Node next;
    
    public Node(T item)
    {
      data = item;
      next = null;
    }
  }
}
