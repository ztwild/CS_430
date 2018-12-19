package hw2.list;
import java.util.NoSuchElementException;

/**
 * Partial implementation of the List interface using 
 * single links and a dummy node. 
 */
public class VersionedList<E>
{
  private final Node head;
  private int count;
  
  public VersionedList()
  {
    head = new Node(null, null);
    count = 0;
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
    Node next;
    volatile int version;

    public Node(E pData, Node pNext)
    {
      data = pData;
      next = pNext;
      version = 0;
    }
  }
  
  /**
   * Implementation of ListIterator for this class
   */
  private class LinkedIterator implements SimpleIterator<E>
  {
    // points to node preceding the next element
    private Node cursor;
    private final int version;
    

    public LinkedIterator()
    {
      cursor = head;
      version = count;
    }
    
    public boolean hasNext()
    {
      return cursor.next != null;
    }
    
    public E next()
    {
      if (!hasNext()) throw new NoSuchElementException();
      while(cursor.next.version > this.version)
    	  cursor = cursor.next; //Skips nodes that are created after iteration created
      
      cursor = cursor.next;
      return cursor.data;
    }
        
    public void add(E item)
    {
      synchronized(cursor){
    	  count++;
    	  Node temp = new Node(item, cursor.next);
          cursor.next = temp;
          cursor = temp;
      }
    }
  }

}
