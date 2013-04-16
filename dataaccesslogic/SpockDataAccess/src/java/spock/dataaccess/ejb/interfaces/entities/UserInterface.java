package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public interface UserInterface extends Serializable {

    Experiment getExperiment();
    String getIPAddress();
    Long getId();
    String getType();
    User getUser();
    void setExperiment(Experiment experiment);
    void setIPAddress(String IPAddress);
    void setId(Long id);
    void setType(String Type);
    void setUser(User user);
    
}
