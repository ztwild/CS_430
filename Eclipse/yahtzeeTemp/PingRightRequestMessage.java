package hw3.yahtzee;

public class PingRightRequestMessage extends AbstractMessage {

  public PingRightRequestMessage(Component sender) {
    super(sender);
  }
  

  @Override
  public void dispatch(Component receiver){
    receiver.handlePingRightRequest(this);
  }
}
