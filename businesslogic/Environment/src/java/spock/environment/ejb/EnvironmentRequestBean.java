package spock.environment.ejb;

import javax.ejb.Stateless;
/**
 *
 * @author Loren Chorley
 */
@Stateless
public class EnvironmentRequestBean implements EnvironmentRequest {
    
    // Login and logout
    
    @Override
    public String returnTestString() {
        return "Spock Environment test string.";
    }
    
}
