package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.entities.Configuration;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface ConfigurationFunctions extends Serializable, BasicEntity<Configuration, String> {
        
}
