package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;
import java.util.Collection;

/**
 *
 * @author loren
 */
public interface User extends Serializable {

    void addUserInterface(UserInterface userInterface);
    String getAccessRights();
    String getEmail();
    Collection<UserInterface> getInterfaces();
    String getPassword();
    String getUsername();
    void removeUserInterface(UserInterface userInterface);
    void setAccessRights(String accessRights);
    void setEmail(String Email);
    void setPassword(String Password);
    void setUsername(String Username);
    
}
