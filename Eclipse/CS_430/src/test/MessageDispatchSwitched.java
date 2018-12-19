package test;

/**
 * Example of ugly code to dispatch messages based on an explicit
 * type attribute in each message.
 *
 */
public class MessageDispatchSwitched
{
  
  public static void main(String[] args)
  {
    Message1 m = new Message1();
    Message2 m2 = new Message2();

    Component c = new Component();
    c.send(m);
    c.send(m2);

    c = new SubComponent();
    c.send(m);
    c.send(m2);

  }
}

/** commented out cause its interfering with DoubleDispatchExample.java

class Component
{
  public void send(IMessage m)
  {
    // Blecch :(
    switch (m.getType()) 
    {
      case IMessage.TYPE1: 
      {
        handleMessage1((Message1) m);
        break;
      }
      case IMessage.TYPE2: 
      {
        handleMessage2((Message2) m);
        break;
      }
      default:
        handleDefault(m);
    }
  }
   
  public void handleDefault(IMessage msg)
  {
    System.out.print("Default handler in base Component, ");
    System.out.println(msg);
  }
  
  public void handleMessage1(Message1 msg)
  {
    System.out.print("Handler 1 in base Component, ");
    System.out.println(msg);
  }
  
  public void handleMessage2(Message2 msg)
  {
    // defer to default handler if not overridden
    handleDefault((IMessage) msg);  
  } 
}

class SubComponent extends Component
{
  @Override
  public void handleMessage2(Message2 msg)  
  {
    System.out.print("Handler 2 in Component2, ");
    System.out.println(msg);
  }
}


interface IMessage
{
  static final int TYPE1 = 1;
  static final int TYPE2 = 2;
  
  int getType();
}

class Message1 implements IMessage
{
  public int getType() {return TYPE1;}
  public String toString()
  {
    return "Message 1";
  }  
}

class Message2 implements IMessage
{
  public int getType() {return TYPE2;}
  public String toString()
  {
    return "Message 2";
  }
}

**/