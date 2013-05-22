package spock.dataaccess.entities;

import java.util.ArrayList;
import java.util.Collection;
import javax.ejb.EJBException;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.User;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Users")
public class UserEntity implements User {
    private static final long serialVersionUID = 1L;
    @Id
    private String Username;
    @NotNull
    protected String Password;
    protected String Email;
    @OneToMany(mappedBy="user")
    protected Collection<UserInterfaceEntity> interfaces;
    protected String accessRights;

    public UserEntity() {
        interfaces = new ArrayList<UserInterfaceEntity>();
    }

    public UserEntity(String Username, String Password, String Email, String accessRights) {
        interfaces = new ArrayList<UserInterfaceEntity>();
        this.Username = Username;
        this.Password = Password;
        this.Email = Email;
        this.accessRights = accessRights;
    }
        
    @Override
    public String getUsername() {
        try {
            
            return Username;
        
        } catch (Exception ex) {
            throw new EJBException("UserEntity.getUsername threw: " + ex.getMessage());
        }
    }

    @Override
    public void setUsername(String Username) {
        try {
            
            this.Username = Username;
        
        } catch (Exception ex) {
            throw new EJBException("UserEntity.setUsername threw: " + ex.getMessage());
        }
    }

    @Override
    public String getPassword() {
        try {
            
            return Password;

        } catch (Exception ex) {
            throw new EJBException("UserEntity.getPassword threw: " + ex.getMessage());
        }
    }

    @Override
    public void setPassword(String Password) {
        try {
            
            this.Password = Password;

        } catch (Exception ex) {
            throw new EJBException("UserEntity.setPassword threw: " + ex.getMessage());
        }
    }

    @Override
    public String getEmail() {
        try {
            
            return Email;

        } catch (Exception ex) {
            throw new EJBException("UserEntity.getEmail threw: " + ex.getMessage());
        }
    }

    @Override
    public void setEmail(String Email) {
        try {
            
            this.Email = Email;

        } catch (Exception ex) {
            throw new EJBException("UserEntity.setEmail threw: " + ex.getMessage());
        }
    }

    @Override
    public Collection<UserInterface> getInterfaces() {
        try {
            
            return (Collection) interfaces;

        } catch (Exception ex) {
            throw new EJBException("UserEntity.getInterfaces threw: " + ex.getMessage());
        }
    }
    
    @Override
    public void addUserInterface(UserInterface userInterface) {
        try {
            
            interfaces.add((UserInterfaceEntity) userInterface);

        } catch (Exception ex) {
            throw new EJBException("UserEntity.addUserInterface threw: " + ex.getMessage());
        }
    }
    
    @Override
    public void removeUserInterface(UserInterface userInterface) {
        try {
            
            interfaces.remove((UserInterfaceEntity) userInterface);

        } catch (Exception ex) {
            throw new EJBException("UserEntity.removeUserInterface threw: " + ex.getMessage());
        }
    }

    @Override
    public String getAccessRights() {
        try {
            
            return accessRights;

        } catch (Exception ex) {
            throw new EJBException("UserEntity.getAccessRights threw: " + ex.getMessage());
        }
    }

    @Override
    public void setAccessRights(String accessRights) {
        try {
            
            this.accessRights = accessRights;

        } catch (Exception ex) {
            throw new EJBException("UserEntity.setAccessRights threw: " + ex.getMessage());
        }
    }

    

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (Username != null ? Username.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof UserEntity)) {
            return false;
        }
        UserEntity other = (UserEntity) object;
        if ((this.Username == null && other.Username != null) || (this.Username != null && !this.Username.equals(other.Username))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.User[ id=" + Username + " ]";
    }
    
}
