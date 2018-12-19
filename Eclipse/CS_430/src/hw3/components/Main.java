package hw3.components;

/**
 * Start up a system with input, client, proxy components.
 * Note that SimpleServer should be running on port 2222.
 */
public class Main
{
  public static void main(String[] args)
  {
    ProxyWorker worker = new ProxyWorker();
    Component proxy = new ProxyComponent(worker);
    Component client = new ClientComponent(proxy);
    Component input = new InputComponent(client);

    proxy.start();
    worker.start();
    client.start();
    input.start();
  }

}
