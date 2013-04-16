package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;

/**
 *
 * @author Loren Chorley
 */
public interface Configuration extends Serializable {
    
    public String getName();
    public void setName(String name);
    public String getConfigValue();
    public void setConfigValue(String configvalue);
    
}
