package test;

/**
 * Failed attempt to dispatch messages based on message type.
 *
 */

//
// Overloaded method selection is done at compile time, based on
// declared type of the arguments. The method with the "most specific" match
// to the declared type of the arguments is chosen from the
// available overloads in the declared type of the target object.
//
// The actual method is selected at run time, based on run time type of the 
// target object.  But remember, the specific *overload* of the method
// based on the argument types has already been fixed by the compiler, even though
// it may be *overridden* by a subtype.
//
// So this doesn't work.  In the send() method, the compile-time type 
// of the message is always IMessage, so the overload of handle() is
// always the general one with argument type IMessage
//
public class MessageDispatchFailed
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
    // this won't work, because it *always* calls handle(Message)
    // based on the compile-time type of m
    handle(m);
  }
  
  public void handle(IMessage msg)
  {
    System.out.print("Default handler in base Component, ");
    System.out.println(msg);
  }
  
  public void handle(Message1 msg)
  {
    System.out.print("Handler 1 in base Component, ");
    System.out.println(msg);
  }
}

class SubComponent extends Component
{
  public void handle(Message2 msg)  
  {
    System.out.print("Handler 2 in Component2, ");
    System.out.println(msg);
  }
  
  // We can override this method, but it doesn't really help
  public void handle(IMessage msg)  
  {
    System.out.print("Handler 2 in Component2, ");
    System.out.println(msg);
  }
}


interface IMessage
{
  
}

class Message1 implements IMessage
{
  public String toString()
  {
    return "Message 1";
  }  
}

class Message2 implements IMessage
{
  public String toString()
  {
    return "Message 2";
  }
}

**/