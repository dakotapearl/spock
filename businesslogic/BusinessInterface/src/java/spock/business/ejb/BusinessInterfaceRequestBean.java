package spock.business.ejb;

import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
@Stateful
public class BusinessInterfaceRequestBean implements BusinessInterfaceRequest {
    
    // Login and logout
    
    @Override
    public String returnTestString() {
        return "Spock Business Interface test string.";
    }
    
}
