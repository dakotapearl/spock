package spock.environment.ejb;

import javax.ejb.Remote;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface EnvironmentRequest {
    public String returnTestString();
}
