/*
@author  j.n.magee 20/11/96
*/
package tools.concurrency;

/* ********************Entry**************************** */
// The definition of entry assumes that there can be many clients
// but only one server

public class Entry<R,P> extends Port<R> {
  private CallMsg<R,P> cm;
  private Port<CallMsg<R,P>> cp = new Port<CallMsg<R,P>>();

  public P call(R req) throws InterruptedException {
    Channel<P> clientChan = new Channel<P>();
    cp.send(new CallMsg<R,P>(req,clientChan));
    return clientChan.receive();
  }

  public R accept() throws InterruptedException {
    cm = cp.receive();
    return cm.request;
  }

  public void reply(P res) throws InterruptedException {
    cm.replychan.send(res);
  }

  @SuppressWarnings("hiding")
  private class CallMsg<R,P> {
    R  request;
    Channel<P> replychan;
    CallMsg(R m, Channel<P> c)
      {request=m; replychan=c;}
  }

}