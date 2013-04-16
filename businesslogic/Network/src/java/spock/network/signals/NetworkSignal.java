package spock.network.signals;

import java.io.Serializable;

/**
 * @author Loren Chorley
 */
public class NetworkSignal implements Serializable {

    private Serializable data;

    public Serializable getData() {
        return data;
    }

    public void setData(Serializable data) {
        this.data = data;
    }
    
}
