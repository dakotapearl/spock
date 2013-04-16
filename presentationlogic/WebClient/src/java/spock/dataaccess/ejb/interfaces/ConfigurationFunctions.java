package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import spock.dataaccess.ejb.interfaces.entities.Configuration;

/**
 *
 * @author Loren Chorley
 */
public interface ConfigurationFunctions extends Serializable, BasicEntity<Configuration, String> {
        
}
