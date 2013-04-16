package spock.business.ejb;

import javax.ejb.Remote;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface BusinessInterfaceRequest {
    public String returnTestString();
}
