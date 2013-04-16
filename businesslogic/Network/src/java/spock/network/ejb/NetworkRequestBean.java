package spock.network.ejb;

import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
@Stateful
public class NetworkRequestBean implements NetworkRequest {
    
    // Login and logout
    
    @Override
    public String returnTestString() {
        return "Spock Network test string.";
    }
    
}
