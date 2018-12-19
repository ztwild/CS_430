package hw2.list;
import java.util.ConcurrentModificationException;
import java.util.NoSuchElementException;

/**
 * Partial implementation of the List interface using 
 * single links and a dummy node. 
 */
public class ImmutableList<E>
{
  private volatile Node head;
  
  public ImmutableList()
  {
    head = new Node(null, null);
  }   
  
  public SimpleIterator<E> listIterator()
  {
    return new LinkedIterator();
  }
  
  /**
   * Node type for this class.
   */
  private class Node
  {
    final E data;
    final Node next;

    public Node(E pData, Node pNext)
    {
      data = pData;
      next = pNext;
    }
  }
  
  /**
   * Implementation of ListIterator for this class
   */
  private class LinkedIterator implements SimpleIterator<E>
  {
    // points to node preceding the next element
    private Node cursor;

    public LinkedIterator()
    {
      cursor = head; 
    }
    
    public boolean hasNext()
    {
      return cursor.next != null;
    }
    
    public E next()
    {
      if (!hasNext()) throw new NoSuchElementException();
      
      cursor = cursor.next;
      return cursor.data;
    }
        
    public synchronized void add(E item)
    {
    	Node result = addRec(head.next, item);
    	if(result == null){
    		throw new ConcurrentModificationException();
    	}
    	head = new Node(null, result);
    	
    	
//      Node temp = new Node(item, cursor.next);
//      cursor.next = temp;
//      cursor = temp;
    }
    
    private Node addRec(Node cur, E item){
    	Node copy = null;
    	if(cur != null){
    		if(cur == cursor){
    			Node temp = new Node(item, cursor.next);
    			copy = new Node(cursor.data, temp);
    			cursor = temp;
    		}else{
    			Node res = addRec(cur.next, item);
    			if(res != null){
    				copy = new Node(cursor.data, res);
    			}
    		}
    	}
    	return copy;
    }
    
  }

}
