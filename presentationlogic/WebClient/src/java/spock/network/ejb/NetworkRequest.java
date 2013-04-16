package spock.network.ejb;

import javax.ejb.Remote;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface NetworkRequest {
    public String returnTestString();
}
